(ns murphy)

(defmacro try!
  "Exactly like try, except that it supports multiple finally clauses
  which will be executed in order, and if any given finally clause
  throws while an exception is already pending, the new exception will
  be suppressed via the Throwable addSuppressed method."
  [& forms]
  (let [[others finals] (split-with #(or (not (list? %))
                                         (not= 'finally (first %)))
                                    forms)]
    (when-not (every? #(= 'finally (first %)) finals)
      (throw
       (RuntimeException. "finally clauses must be last")))
    (loop [[[_finally_ & fin-body] & finals] finals
           expansion `(try ~@others)]
      (if-not _finally_
        expansion
        (recur finals
               `(let [fin# (fn [] ~@fin-body)
                      result# (try
                                ~expansion
                                (catch Throwable ex#
                                  (try
                                    (fin#)
                                    (catch Throwable ex2#
                                      (.addSuppressed ex# ex2#)))
                                  (throw ex#)))]
                  (fin#)
                  result#))))))

(defn- validate-with-final-bindings [bindings]
  (assert (vector? bindings))
  (loop [bindings bindings]
    (case (count bindings)
      0 true
      2 true  ; final "name init" pair
      (1 3) (throw (RuntimeException. "Unexpected end of with-final bindings"))
      (let [[_binding _init maybe-kind & remainder] bindings]
        (if-not (#{:always :error} maybe-kind)
          (recur (cons maybe-kind remainder))
          (let [[_action & remainder] remainder]
            (recur remainder)))))))

(defmacro with-final
  "The bindings must be a vector of elements, each of which is either
  \"binding init\", \"binding init :always action\", or \"binding
  init :error action\". Binds each binding to the value of the
  corresponding init just as let would, and behaves as if each init
  value were guarded by a nested try! form that calls action on the
  value in a finally clause when :always is specified, or action on
  the value in a Throwable handler when :error is specified.
  Suppresses any exceptions thrown by the actions via the Throwable
  addSuppressed method. Type hints should be on the init forms, not
  the names."
  [bindings & body]
  (validate-with-final-bindings bindings)
  (if (empty? bindings)
    `(do ~@body)
    (if (= 2 (count bindings))
      ;; "name init"
      `(let ~bindings ~@body)
      ;; either "name init" or "name init kind action"
      (let [kind (nth bindings 2 nil)]
        (if-not (#{:always :error} kind)
          `(let ~(subvec bindings 0 2)
             (with-final ~(subvec bindings 2)
               ~@body))
          (let [[bind init] bindings
                action (nth bindings 3)]
            (case kind
              :always `(let [val# ~init
                             ~bind val#
                             finalize# (fn [] (~action val#))]
                         (let [result# (try
                                         (with-final ~(subvec bindings 4)
                                           ~@body)
                                         (catch Throwable ex#
                                           (try
                                             (finalize#)
                                             (catch Throwable ex2#
                                               (.addSuppressed ex# ex2#)))
                                           (throw ex#)))]
                           (finalize#)
                           result#))
              :error `(let [val# ~init
                            ~bind val#]
                        (try
                          (with-final ~(subvec bindings 4)
                            ~@body)
                          (catch Throwable ex#
                            (try
                              (~action val#)
                              (catch Throwable ex2#
                                (.addSuppressed ex# ex2#)))
                            (throw ex#)))))))))))

(defmacro with-open!
  "Bindings must be a vector of [binding init ...] pairs.  Binds each
  binding to the value of the corresponding init just as let would,
  and behaves as if each value were guarded by a nested try form that
  calls .close on the value in a finally clause.  Suppresses any
  exceptions thrown by the .close calls via the Throwable
  addSuppressed method. Type hints should be on the init forms, not
  the names."
  [bindings & body]
  (assert (vector? bindings))
  (if (empty? bindings)
    `(do ~@body)
    (do
      (assert (even? (count bindings)))
      (let [bindings (vec (mapcat #(concat % '(:always .close))
                                  (partition 2 bindings)))]
        `(with-final ~bindings
           ~@body)))))

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
      (let [[name init maybe-kind & remainder] bindings]
        (if-not (#{:always :error} maybe-kind)
          (recur (cons maybe-kind remainder))
          (let [[action & remainder] remainder]
            (recur remainder)))))))

(defmacro with-final
  "The bindings must be a vector of elements, each of which is either
  \"name init\", \"name init :always action\", or \"name init :error
  action\".  Binds each name to the value of the corresponding init,
  and behaves exactly as if each subsequent name were guarded by a
  nested try! form that calls (action name) in its finally clause
  when :always is specified, or (action name) in a Throwable handler
  when :error is specified.  Suppresses any exceptions thrown by the
  actions via the Throwable addSuppressed method."
  [bindings & body]
  (validate-with-final-bindings bindings)
  (if (empty? bindings)
    `(do ~@body)
    (if (= 2 (count bindings))
      ;; "name init"
      (let [[bind init] bindings]
        `(let [~bind ~init]
           ~@body))
      ;; either "name init" or "name init kind action"
      (let [[bind init maybe-kind maybe-action] bindings]
        (if-not (#{:always :error} maybe-kind)
          `(let [~bind ~init]
             (with-final ~(subvec bindings 2)
               ~@body))
          (let [action maybe-action
                kind maybe-kind]
            (case kind
              :always `(let [finalize# (fn [x#] (~action x#))
                             val# ~init
                             ~bind val#]
                         (let [result# (try
                                         (with-final ~(subvec bindings 4)
                                           ~@body)
                                         (catch Throwable ex#
                                           (try
                                             (finalize# val#)
                                             (catch Throwable ex2#
                                               (.addSuppressed ex# ex2#)))
                                           (throw ex#)))]
                           (finalize# val#)
                           result#))
              :error `(let [cleanup# (fn [x#] (~action x#))
                            val# ~init
                            ~bind val#]
                        (try
                          (with-final ~(subvec bindings 4)
                            ~@body)
                          (catch Throwable ex#
                            (try
                              (cleanup# val#)
                              (catch Throwable ex2#
                                (.addSuppressed ex# ex2#)))
                            (throw ex#)))))))))))

(defmacro with-open!
  "Bindings must be a vector of [name init ...] pairs.  Binds each
  name to the value of the corresponding init, and behaves exactly as
  if each subsequent name were guarded by a nested try form that
  calls (.close name) in its finally clause.  Suppresses any
  exceptions thrown by the .close calls via the Throwable
  addSuppressed method."
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

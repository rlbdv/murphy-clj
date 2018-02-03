(ns murphy)

(defmacro try!
  "Exactly like try, except that if the finally clause throws anything
  while an exception is already pending, the new exception will be
  suppressed via the Throwable addSuppressed method."
  [& forms]
  (let [[others finals] (split-with #(or (not (list? %))
                                         (not= 'finally (first %)))
                                    forms)]
    (case (count finals)
      0 `(try ~@others)
      1 (let [[_finally_ & fin-body] (first finals)]
          `(let [fin# (fn [] ~@fin-body)
                 result# (try
                           ~@others
                           (catch Throwable ex#
                             (try
                               (fin#)
                               (catch Throwable ex2#
                                 (.addSuppressed ex# ex2#)))
                             (throw ex#)))]
             (fin#)
             result#))
      (throw
       (RuntimeException. "finally clause must be last and unique")))))

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
      (case (count bindings)
        2 (let [[var init] bindings]
            `(let [~var ~init]
               ~@body)))
      ;; either "name init" or "name init kind action"
      (let [[var init maybe-kind maybe-action] bindings]
        (if-not (#{:always :error} maybe-kind)
          `(let [~var ~init]
             (with-final ~(subvec bindings 2)
               ~@body))
          (let [action maybe-action
                kind maybe-kind]
            (case kind
              :always `(let [finalize# (fn [x#] (~action x#))
                             ~var ~init]
                         (let [result# (try
                                         (with-final ~(subvec bindings 4)
                                           ~@body)
                                         (catch Throwable ex#
                                           (try
                                             (finalize# ~var)
                                             (catch Throwable ex2#
                                               (.addSuppressed ex# ex2#)))
                                           (throw ex#)))]
                           (finalize# ~var)
                           result#))
              :error `(let [cleanup# (fn [x#] (~action x#))
                            ~var ~init]
                        (try
                          (with-final ~(subvec bindings 4)
                            ~@body)
                          (catch Throwable ex#
                            (try
                              (cleanup# ~var)
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

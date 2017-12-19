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

(defmacro with-final
  "Bindings must be a vector of [name init finalize ...] triples.
  Binds each name to the value of the corresponding init, and behaves
  exactly as if each subsequent name were guarded by a nested try form
  that calls (finalize name) in its finally clause.  Suppresses any
  exceptions thrown by the finalize calls via the Throwable
  addSuppressed method."
  [bindings & body]
  (assert (vector? bindings))
  (if (empty? bindings)
    `(do ~@body)
    (do
      (assert (zero? (mod (count bindings) 3)))
      (assert (every? symbol? (take-nth 3 bindings)))
      (let [[var init finalize] bindings]
        `(let [finalize# (fn [x#] (~finalize x#))
               ~var ~init]
           (let [result# (try
                           (with-final ~(subvec bindings 3)
                             ~@body)
                           (catch Throwable ex#
                             (try
                               (finalize# ~var)
                               (catch Throwable ex2#
                                 (.addSuppressed ex# ex2#)))
                             (throw ex#)))]
             (finalize# ~var)
             result#))))))

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
      (let [bindings (vec (mapcat #(concat % '(.close)) (partition 2 bindings)))]
        `(with-final ~bindings
           ~@body)))))

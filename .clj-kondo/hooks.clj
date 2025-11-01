(ns hooks
  (:require
   [clj-kondo.hooks-api :as api :refer [list-node token-node vector-node]]))

(defn try!
  [{{[_ & body] :children} :node}]
  ;; Rewrite
  ;;   (try body-without-finals (finally 1) (finally 2)
  ;; as
  ;;   (try (try (try & body-without-finals) (finally 1) (finally 2)))
  (let [[others finals] (split-with #(or (not (list? %))
                                         (not= 'finally (first %)))
                                    body)]
    {:node (list-node
            (loop [result (list* (token-node 'try) others)
                   [final & finals] finals]
              (if-not final
                result
                (recur (list (token-node 'try) result final)
                       finals))))}))

(defn with-open!
  [{{[_ & body] :children} :node}]
  ;; Rewrite as with-open since :lint-as with-open didn't work
  {:node (list-node (list* (token-node 'with-open) body))})

(defn with-final
  [{{[_ bindings & body] :children} :node}]
  ;; Strip the :error and :always args and rewrite as let.
  (let [basic-bindings (loop [bindings (:children bindings)
                              result []]
                         (case (count bindings)
                           0 result
                           2 (apply conj result bindings)
                           (1 3) (throw (ex-info "Unexpected end of with-final bindings" {}))
                           (let [[name init maybe-kind & others] bindings]
                             (if-not (#{:always :error} maybe-kind)
                               (recur (cons maybe-kind others)
                                      (conj result
                                            name
                                            init))
                               (let [[action & others] others]
                                 (recur others
                                        (conj result
                                              name
                                              init
                                              (token-node '_)
                                              action)))))))]
    {:node (list-node
            (list* (token-node 'let)
                   (vector-node basic-bindings)
                   body))}))

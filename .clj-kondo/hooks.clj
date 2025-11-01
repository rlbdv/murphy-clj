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

(ns murphy-test
  (:require
   [clojure.test :refer :all]
   [murphy :refer [try! with-final with-open!]]))

(deftest suppressing-try
  (is (= nil (try!)))
  (is (= 1 (try! 1)))
  (is (= 3 (try! 1 2 3)))
  (is (= nil (try! (finally))))
  (is (= 1 (try! 1 (finally))))
  (let [fin (atom [])]
    (is (= nil (try!
                 (finally
                   (swap! fin conj 1)))))
    (is (= [1] @fin)))
  (let [fin (atom [])]
    (is (= nil (try!
                 (catch Exception ex
                   (swap! fin conj 1))
                 (finally
                   (swap! fin conj 2)))))
    (is (= [2] @fin)))
  (let [fin (atom [])]
    (is (= [1] (try!
                 (throw (Exception. "one"))
                 (catch Exception ex
                   (swap! fin conj 1))
                 (finally
                   (swap! fin conj 2)))))
    (is (= [1 2] @fin)))
  (let [fin (atom [])]
    (is (= nil (try!
                 (finally
                   (swap! fin conj 1)
                   (swap! fin conj 2)))))
    (is (= [1 2] @fin)))
  (let [fin (atom [])]
    (is (= 1 (try!
               (inc 0)
               (finally
                 (swap! fin conj 1)))))
    (is (= [1] @fin)))
  (let [fin (atom [])
        ex-1 (Exception. "one")]
    (try
      (try!
        (throw ex-1)
        (finally
          (swap! fin conj 1)))
      (catch Exception ex
        (is (= ex-1 ex))))
    (is (= [1] @fin)))
  (let [fin (atom [])
        ex-1 (Exception. "one")
        ex-2 (Exception. "two")]
    (try
      (try!
        (throw ex-1)
        (finally
          (swap! fin conj 1)
          (throw ex-2)))
      (catch Exception ex
        (is (= ex-1 ex))
        (is (= [ex-2] (seq (.getSuppressed ex))))))
    (is (= [1] @fin))))

(deftest multi-finally
  (let [fin (atom [])
        ex-1 (Exception. "one")
        ex-2 (Exception. "two")
        ex-3 (Exception. "three")]
    (try
      (try!
        (throw ex-1)
        (finally
          (swap! fin conj 1)
          (throw ex-2))
        (finally
          (swap! fin conj 2)
          (throw ex-3)))
      (catch Exception ex
        (is (= ex-1 ex))
        (is (= [ex-2 ex-3] (seq (.getSuppressed ex))))))
    (is (= [1 2] @fin))))

(defrecord CloseableThing [close-this]
  java.lang.AutoCloseable
  (close [this] (close-this this)))

(deftest closeable-thing-behavior
  (let [closed? (atom false)
        closeable (->CloseableThing (fn [this] (reset! closed? true)))]
    (is (= false @closed?))
    (with-open [x closeable]
      (is (= false @closed?))
      :foo)
    (is (= true @closed?))))

(deftest with-final-always-behavior
  (is (= nil (with-final [])))
  (is (= 1 (with-final [] 1)))

  (testing "when nothing is thrown"
    (let [closed? (atom false)
          closeable (->CloseableThing (fn [this] (reset! closed? true)))]
      (is (= false @closed?))
      (is (= :foo (with-final [x closeable :always .close]
                    (is (= false @closed?))
                    (is (= x closeable))
                    :foo)))
      (is (= true @closed?)))

    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= :foo (with-final [x closeable-1 :always .close
                               y closeable-2 :always .close]
                    (is (=  [] @closes))
                    (is (= x closeable-1))
                    (is (= y closeable-2))
                    :foo)))
      (is (= [2 1] @closes))))

  (testing "when body throws"
    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= ["bar" {::bar 1}]
             (try
               (with-final [x closeable-1 :always .close
                            y closeable-2 :always .close]
                 (is (=  [] @closes))
                 (throw (ex-info "bar" {::bar 1})))
               (catch clojure.lang.ExceptionInfo ex
                 [(.getMessage ex) (ex-data ex)]))))
      (is (= [2 1] @closes))))

  (testing "when close throws"
    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this]
                                          (swap! closes conj 2)
                                          (throw (ex-info "bar" {::bar 1}))))
          closeable-3 (->CloseableThing (fn [this] (swap! closes conj 3)))]
      (let [ex (try
                 (with-final [x closeable-1 :always .close
                              y closeable-2 :always .close
                              z closeable-3 :always .close]
                   (is (=  [] @closes))
                   :foo)
                 (catch clojure.lang.ExceptionInfo ex
                   ex))]
        (is (= [3 2 1] @closes))
        (is (= ["bar" {::bar 1}] [(.getMessage ex) (ex-data ex)]))
        (is (= nil (seq (.getSuppressed ex)))))))

  (testing "when body and close throw"
    (let [closes (atom [])
          close-ex-1 (ex-info "bar" {::bar 1})
          close-ex-2 (ex-info "baz" {::baz 1})
          body-ex (ex-info "bax" {::bax 1})
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this]
                                          (swap! closes conj 2)
                                          (throw close-ex-1)))
          closeable-3 (->CloseableThing (fn [this]
                                          (swap! closes conj 3)
                                          (throw close-ex-2)))]
      (let [ex (try
                 (with-final [x closeable-1 :always .close
                              y closeable-2 :always .close
                              z closeable-3 :always .close]
                   (is (=  [] @closes))
                   (throw body-ex))
                 (catch clojure.lang.ExceptionInfo ex
                   ex))]
        (is (= [3 2 1] @closes))
        (is (= ["bax" {::bax 1}] [(.getMessage ex) (ex-data ex)]))
        (is (= [close-ex-2 close-ex-1]
               (seq (.getSuppressed ex))))))))

(deftest with-final-destructuring
  (is (= [2 1] (with-final [[x y] [1 2]]
                 [y x])))
  (is (= [3 1] (with-final [[x] [1 2]
                            [y] [3 4]]
                 [y x])))
  (is (= [2 1]
         (with-final [[x y :as v] [1 2] :always #(is (= [1 2] %))]
           [y x]))))

(deftest with-final-error-behavior
  (is (= nil (with-final [])))
  (is (= 1 (with-final [] 1)))

  (testing "when nothing is thrown"
    (let [closed? (atom false)
          closeable (->CloseableThing (fn [this] (reset! closed? true)))]
      (is (= false @closed?))
      (is (= :foo (with-final [x closeable :error .close]
                    (is (= false @closed?))
                    (is (= x closeable))
                    :foo)))
      (is (= false @closed?)))

    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= :foo (with-final [x closeable-1 :error .close
                               y closeable-2 :error .close]
                    (is (=  [] @closes))
                    (is (= x closeable-1))
                    (is (= y closeable-2))
                    :foo)))
      (is (= [] @closes))))

  (testing "when body throws"
    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= ["bar" {::bar 1}]
             (try
               (with-final [x closeable-1 :error .close
                            y closeable-2 :error .close]
                 (is (=  [] @closes))
                 (throw (ex-info "bar" {::bar 1})))
               (catch clojure.lang.ExceptionInfo ex
                 [(.getMessage ex) (ex-data ex)]))))
      (is (= [2 1] @closes))))

  (testing "when only a close throws"
    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this]
                                          (swap! closes conj 2)
                                          (throw (ex-info "bar" {::bar 1}))))
          closeable-3 (->CloseableThing (fn [this] (swap! closes conj 3)))]
      (let [result (try
                     (with-final [x closeable-1 :error .close
                                  y closeable-2 :error .close
                                  z closeable-3 :error .close]
                       (is (=  [] @closes))
                       :foo)
                     (catch clojure.lang.ExceptionInfo ex
                       ex))]
        (is (= [] @closes))
        (is (= :foo result)))))

  (testing "when body and close throw"
    (let [closes (atom [])
          close-ex-1 (ex-info "bar" {::bar 1})
          close-ex-2 (ex-info "baz" {::baz 1})
          body-ex (ex-info "bax" {::bax 1})
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this]
                                          (swap! closes conj 2)
                                          (throw close-ex-1)))
          closeable-3 (->CloseableThing (fn [this]
                                          (swap! closes conj 3)
                                          (throw close-ex-2)))]
      (let [ex (try
                 (with-final [x closeable-1 :error .close
                              y closeable-2 :error .close
                              z closeable-3 :error .close]
                   (is (=  [] @closes))
                   (throw body-ex))
                 (catch clojure.lang.ExceptionInfo ex
                   ex))]
        (is (= [3 2 1] @closes))
        (is (= ["bax" {::bax 1}] [(.getMessage ex) (ex-data ex)]))
        (is (= [close-ex-2 close-ex-1]
               (seq (.getSuppressed ex))))))))

(deftest with-final-mixed-forms

  (is (= 1 (with-final [x 1] x)))

  (testing "normal let bindings and :error"
    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= ["bar" {::bar 1}]
             (try
               (with-final [c1 closeable-1
                            x c1 :error .close
                            c2 closeable-2
                            y c2 :error .close]
                 (is (=  [] @closes))
                 (throw (ex-info "bar" {::bar 1})))
               (catch clojure.lang.ExceptionInfo ex
                 [(.getMessage ex) (ex-data ex)]))))
      (is (= [2 1] @closes))))

  (testing "normal let bindings and :always"
    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= ["bar" {::bar 1}]
             (try
               (with-final [c1 closeable-1
                            x c1 :always .close
                            c2 closeable-2
                            y c2 :always .close]
                 (is (=  [] @closes))
                 (throw (ex-info "bar" {::bar 1})))
               (catch clojure.lang.ExceptionInfo ex
                 [(.getMessage ex) (ex-data ex)]))))
      (is (= [2 1] @closes)))))

(deftest suppressing-open
  ;; As long as this trivially is based on with-final, rely on its
  ;; tests for much of the work now.
  (is (= nil (with-open! [])))
  (is (= 1 (with-open! [] 1)))
  (testing "closeable thing"
    (let [closed? (atom false)
          closeable (->CloseableThing (fn [this] (reset! closed? true)))]
      (is (= false @closed?))
      (with-open [x closeable]
        (is (= false @closed?))
        :foo)
      (is (= true @closed?)))))

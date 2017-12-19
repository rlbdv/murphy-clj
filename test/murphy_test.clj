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

(defrecord CloseableThing [close-this]
  java.lang.AutoCloseable
  (close [this] (close-this this)))

(deftest with-final-behavior
  (is (= nil (with-final [])))
  (is (= 1 (with-final [] 1)))

  (testing "closeable thing"
    (let [closed? (atom false)
          closeable (->CloseableThing (fn [this] (reset! closed? true)))]
      (is (= false @closed?))
      (with-open [x closeable]
        (is (= false @closed?))
        :foo)
      (is (= true @closed?))))

  (testing "when nothing is thrown"
    (let [closed? (atom false)
          closeable (->CloseableThing (fn [this] (reset! closed? true)))]
      (is (= false @closed?))
      (is (= :foo (with-final [x closeable .close]
                    (is (= false @closed?))
                    (is (= x closeable))
                    :foo)))
      (is (= true @closed?)))

    (let [closes (atom [])
          closeable-1 (->CloseableThing (fn [this] (swap! closes conj 1)))
          closeable-2 (->CloseableThing (fn [this] (swap! closes conj 2)))]
      (is (=  [] @closes))
      (is (= :foo (with-final [x closeable-1 .close
                               y closeable-2 .close]
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
               (with-final [x closeable-1 .close
                            y closeable-2 .close]
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
                 (with-final [x closeable-1 .close
                              y closeable-2 .close
                              z closeable-3 .close]
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
                 (with-final [x closeable-1 .close
                              y closeable-2 .close
                              z closeable-3 .close]
                   (is (=  [] @closes))
                   (throw body-ex))
                 (catch clojure.lang.ExceptionInfo ex
                   ex))]
        (is (= [3 2 1] @closes))
        (is (= ["bax" {::bax 1}] [(.getMessage ex) (ex-data ex)]))
        (is (= [close-ex-2 close-ex-1]
               (seq (.getSuppressed ex))))))))

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

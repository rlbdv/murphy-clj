(defproject murphy "0.5.3"
  :description "Clojure library for better handling of bad situations."
  :url "https://gitlab.com/clj-murphy/murphy"
  :licenses [{:name "GNU Lesser General Public License, version 2.1 or newer"
              :url "https://www.gnu.org/licenses/lgpl-2.1.html"}
             {:name "Eclipse Public License 1.0 or newer"
              :url "http://www.eclipse.org/legal/epl-v10.html"}]
  :dependencies [[org.clojure/clojure "1.12.3"]]
  :profiles {:eastwood {:plugins [[jonase/eastwood "1.4.3"]]}
             :kondo {:dependencies [[clj-kondo "2025.10.23"]]}}
  :eastwood {:config-files ["eastwood.clj"]}
  :aliases {"eastwood" ["with-profile" "+eastwood" "eastwood"]
            "kondo" ["with-profile" "+kondo" "run" "-m"
                     "clj-kondo.main" "--parallel" "--lint" "src" "test"]
            "check-all" ["do" "check," "kondo," "eastwood," "test"]})

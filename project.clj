(defproject murphy "0.5.2-SNAPSHOT"
  :description "Clojure library for better handling of bad situations."
  :url "https://gitlab.com/clj-murphy/murphy"
  :licenses [{:name "GNU Lesser General Public License, version 2.1 or newer"
              :url "https://www.gnu.org/licenses/lgpl-2.1.html"}
             {:name "Eclipse Public License 1.0 or newer"
              :url "http://www.eclipse.org/legal/epl-v10.html"}]
  :dependencies [[org.clojure/clojure "1.10.2"]]
  :profiles {:eastwood {:plugins [[jonase/eastwood "0.9.7"]]}}
  :eastwood {:config-files ["eastwood.clj"]}
  :aliases {"eastwood" ["with-profile" "+eastwood" "eastwood"]
            "check-all" ["do" "check," "eastwood," "test"]})

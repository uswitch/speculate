(def +project+ 'speculate)
(def +version+ "0.3.0-SNAPSHOT")

(def dependencies
  '[[org.clojure/clojure "1.9.0-alpha14"]])

(def dev-dependencies
  '[[org.clojure/test.check "0.9.0"]
    [clj-time "0.13.0"]])

(set-env! :dependencies   dependencies
          :source-paths   #{"src"}
          :resource-paths #{"src"}
          :exclusions     '[org.clojure/clojure org.clojure/test.check])

(let [ci-user (System/getenv "TRAVIS_USER")
      ci-pass (System/getenv "TRAVIS_PASS")]
  (when (and ci-user ci-pass)
    (set-env! :repositories
              (-> (into {} (get-env :repositories))
                  (update "clojars" assoc
                          :username ci-user
                          :password ci-pass)
                  (into [])))))

(task-options!
 pom {:project +project+
      :version +version+
      :license {"Eclipse Public License"
                "http://www.eclipse.org/legal/epl-v10.html"}}
 push {:repo "clojars"})

(deftask dev
  "Dev profile"
  []
  (set-env! :dependencies #(vec (concat % dev-dependencies))
            :source-paths #(conj % "dev" "test"))
  (fn [next-handler]
    (fn [fs]
      (next-handler fs))))

(defn cider? []
  (get (ns-publics 'boot.user) 'cider))

(ns-unmap 'boot.user 'test)
(deftask test []
  (set-env!
   :dependencies #(conj % '[adzerk/boot-test "1.1.2"])
   :source-paths #(conj % "test"))
  (require 'adzerk.boot-test)
  (comp (dev) ((resolve 'adzerk.boot-test/test))))

(replace-task!
 [r repl] (comp ((or (cider?) (constantly identity))) (dev) r))

(deftask deploy []
  (comp (pom) (jar) (install) (push)))

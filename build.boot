(def +project+ 'speculate)
(def +version+ "0.1.0-SNAPSHOT")

(def dependencies
  '[[org.clojure/clojure "1.9.0-alpha14"]])

(def dev-dependencies
  '[[org.clojure/test.check "0.9.0"]
    [clj-time "0.13.0"]])

(set-env! :dependencies   dependencies
          :source-paths   #{"src"}
          :resource-paths #{"src" "resources"}
          :exclusions     '[org.clojure/clojure org.clojure/test.check])

(task-options!
 pom {:project +project+
      :version +version+
      :license {"Eclipse Public License"
                "http://www.eclipse.org/legal/epl-v10.html"}}
 push {:repo "releases"})

(deftask dev
  "Dev profile"
  []
  (set-env! :dependencies #(vec (concat % dev-dependencies))
            :source-paths #(conj % "dev" "test")
            :resource-paths #(conj % "test-resources"))
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
  (comp (dev) ((resolve 'adzerk.boot-test/test) "-n" "speculate.matcher-test")))

(replace-task!
 [r repl] (comp ((or (cider?) (constantly identity))) (dev) r))

(deftask deploy []
  (comp (pom) (jar) (install) (push)))

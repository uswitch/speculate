# Speculate

Speculate is a library that extends `clojure.spec` and interprets what
`clojure.spec`s mean. It can translate specs into different formats,
whilst retaining all the features and power of `clojure.spec`.

It does this by analysing `clojure.spec` forms, and building an
abstract syntax tree (AST) as an intermediary form, which can then be
rendered into different formats. Included formats are `json-schema`
and swagger's "openapi specification".

Speculate understands regular `clojure.spec` specs, but currently
`clojure.spec`s do not provide enough informtation to render highly
descriptive `json-schema` documents, or swagger-like formats.

Speculate provides a way to decorate existing `clojure.spec` forms
with arbitrary information, giving control of what is output to the
user.

Speculate produces Clojure datastructures that can be serialized to
any serialization format (such as json), with a relevant library
(such as [cheshire](https://github.com/dakrone/cheshire)).

## Status

The status of this project is pre-alpha. It works for our case. the
API is subject to change. We're working towards a stable API.

## Artifacts

`speculate` artifacts are [released to Clojars](https://clojars.org/speculate).

If you are using Maven, add the following repository definition to your `pom.xml`:

``` xml
<repository>
  <id>clojars.org</id>
  <url>http://clojars.org/repo</url>
</repository>
```

### The Most Recent Release

With Leiningen/Boot:

``` clj
;; In active development
[speculate "0.1.0-SNAPSHOT"]

```

## Usage

### Usage with default `clojure.spec`s

``` clojure

(require '[clojure.spec :as s])

(s/def ::color #{"red" "green" "yellow"})
(s/def ::diameter pos-int?)
(s/def ::description string?)
(s/def ::apple (s/keys :req-un [::color ::diameter] :opt-un [::description]))

(require '[speculate.json-schema :as js])
(require '[speculate.ast :as ast])

(js/schema (ast/parse ::apple))

=> {:type object,
    :properties {"color" {:enum #{"yellow" "green" "red"},
                          :type {:type string}},
                 "diameter" {:type integer, :format int32, :minimum 1},
                 "description" {:type string}},
    :required ["color" "diameter"],
    :title "Apple"}

```

### Generating detailed `json-schema`


``` clojure

(require '[clojure.spec :as s])
(require '[speculate.spec :as u])

(s/def ::color #{"red" "green" "yellow"})

(s/def ::diameter
  (u/spec
   :description "Diameter of an apple in millimetres."
   :spec pos-int?
   :maximum 300))

(s/def ::description string?)

(s/def ::apple
  (u/spec
   :description "The fruit of the apple tree."
   :spec (s/keys :req-un [::color ::diameter] :opt-un [::description])))

(s/def ::apples (u/set-of ::apple))

(s/def ::apple-tree
  (u/spec
   :description "The apple tree (Malus pumila, commonly and
                 erroneously called Malus domestica) is a deciduous
                 tree in the rose family best known for its sweet,
                 pomaceous fruit, the apple."
   :spec (s/keys :req-un [::apples])))

(require '[speculate.json-schema :as js])

(js/schema ::apple)

=> {:properties {"apples" {:type array
                           :items {:properties {"color" {:enum #{"yellow" "green" "red"}
                                                         :type {:type string}}
                                                "diameter" {:type integer
                                                            :minimum 1
                                                            :title "Diameter"
                                                            :description "Diameter of an apple in millimetres."
                                                            :maximum 300}
                                                "description" {:type string}}
                                   :type object
                                   :required ["color" "diameter"]
                                   :title "Apple"
                                   :description "The fruit of the apple tree."}}}
    :type object
    :required ["apples"]
    :title "AppleTree"
    :description "The apple tree (Malus pumila commonly and erroneously
                  called Malus domestica) is a deciduous tree in the
                  rose family best known for its sweet pomaceous fruit
                  the apple."}

(js/schema ::apple-tree :extract-definitions? true)

=> {"AppleTree" {:properties {"apples" {:type array
                                        :items {"$ref" "#!/definitions/Apple"}}}
                 :type object
                 :required ["apples"]
                 :title "AppleTree"
                 :description "The apple tree (Malus pumila commonly and
                               erroneously called Malus domestica) is a
                               deciduous tree in the rose family best
                               known for its sweet pomaceous fruit the apple."}
    :definitions {"Diameter" {:type integer
                              :minimum 1
                              :title "Diameter"
                              :description "Diameter of an apple in millimetres."
                              :maximum 300}
                  "Apple" {:properties {"color" {:enum #{"yellow" "green" "red"}
                                                 :type {:type string}}
                                        "diameter" {"$ref" "#!/definitions/Diameter"}
                                        "description" {:type string}}
                           :type object
                           :required ["color" "diameter"]
                           :title "Apple"
                           :description "The fruit of the apple tree."}}}

```

## License

Distributed under the Eclipse Public License, the same as Clojure.

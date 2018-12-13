(defproject com.breezeehr/hitch2 "0.2.1-SNAPSHOT"
  :description ""
  ;; lein-tools-deps plugin does not work with lein 2.7.1
  :min-lein-version "2.8.0"
  :url "https://github.com/Breezeemr/hitch2/tree/master/"
  :repositories [["snapshots" {:url "s3p://breezepackages/snapshots" :creds :gpg}]
                 ["releases" {:url "s3p://breezepackages/releases" :creds :gpg}]]
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]})

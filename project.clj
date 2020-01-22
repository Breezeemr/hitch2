(defproject com.breezeehr/hitch2 "0.4.0-SNAPSHOT"
  :description ""
  :url "https://github.com/Breezeemr/hitch2/tree/master/"
  :repositories [["snapshots" {:url "s3p://breezepackages/snapshots" :creds :gpg}]
                 ["releases" {:url "s3p://breezepackages/releases" :creds :gpg}]]
  :source-paths ["src" "src-java"]
  :jar-exclusions [#".+\.java$"]
  :dependencies
  [[org.clojure/clojure "1.9.0"]
   [org.clojure/clojurescript "1.10.339"]
   [rads/anomalies "0.1.13"]])

(defproject ski_solution "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ski-solution.core
  :java-source-paths ["java-src"]
  :jvm-opts ["-Xms1g" "-Xmx8g" "-XX:+HeapDumpOnOutOfMemoryError"]
  :dependencies [[org.clojure/clojure "1.8.0"]])

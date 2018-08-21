(ns straw-interceptor.core
  "Implementation of the interceptor pattern.")

(defn- execute-steps
  [ctx]
  (if (contains? ctx :error)
    (do
      (if-let [interceptor (peek (:stack ctx))]
        (let [ctx' (-> ctx (update :stack pop))]
          (if-let [error (:error interceptor)]
            (recur (error ctx'))
            (recur ctx')))
        (throw (:error ctx))))
    (if-let [interceptor (peek (:queue ctx))]
      (let [ctx' (-> ctx
                     (update :queue pop)
                     (update :stack conj interceptor))]
        (if-let [enter (:enter interceptor)]
          (recur (try
                   (enter ctx')
                   (catch Exception e
                     (assoc ctx' :error e))))
          (recur ctx')))
      (if-let [interceptor (peek (:stack ctx))]
        (let [ctx' (-> ctx (update :stack pop))]
          (if-let [leave (:leave interceptor)]
            (recur (leave ctx'))
            (recur ctx')))
        ctx))))

(defn execute
  "Execute an interceptor chain."
  [chain ctx]
  (let [ctx (assoc ctx :queue (vec (reverse chain)) :stack [])]
    (-> (execute-steps ctx)
        (dissoc :queue :stack))))

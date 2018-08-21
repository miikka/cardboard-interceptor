(ns straw-interceptor.core
  "Implementation of the interceptor pattern.")

(defn- try-f
  [f ctx]
  (if f
    (try
      (f ctx)
      (catch Exception e
        (assoc ctx :error e)))
    ctx))

(defn- execute-ctx
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
        (recur (try-f (:enter interceptor) ctx')))
      (if-let [interceptor (peek (:stack ctx))]
        (let [ctx' (-> ctx (update :stack pop))]
          (recur (try-f (:leave interceptor) ctx')))
        ctx))))

(defn execute
  "Execute an interceptor chain."
  [chain ctx]
  (let [ctx (assoc ctx :queue (vec (reverse chain)) :stack [])]
    (-> (execute-ctx ctx)
        (dissoc :queue :stack))))

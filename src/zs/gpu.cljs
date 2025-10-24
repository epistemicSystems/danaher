(ns zs.gpu
  (:require [promesa.core :as p]))

(def shader-source
  "struct Params {\n     alpha : f32;\n     low : f32;\n     high : f32;\n     dt : f32;\n };\n\n @group(0) @binding(0) var<storage, read> inputSample : array<f32, 1>;\n @group(0) @binding(1) var<storage, read_write> bandState : array<f32, 2>;\n @group(0) @binding(2) var<storage, read_write> outputSample : array<f32, 1>;\n @group(0) @binding(3) var<uniform> params : Params;\n\n @compute @workgroup_size(1)\n fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n   if (gid.x > 0u) {\n     return;\n   }\n   let x = inputSample[0];\n   var lowState = bandState[0];\n   var highState = bandState[1];\n   let aLow = exp(-params.low * params.dt);\n   let aHigh = exp(-params.high * params.dt);\n   lowState = aLow * lowState + (1.0 - aLow) * x;\n   highState = aHigh * highState + (1.0 - aHigh) * x;\n   bandState[0] = lowState;\n   bandState[1] = highState;\n   outputSample[0] = (highState - lowState) * params.alpha;\n }")

(defn- create-buffer
  [device usage size data]
  (let [buffer (.createBuffer device
                              (clj->js
                               (cond-> {:size size
                                        :usage usage}
                                 data (assoc :mappedAtCreation true))))]
    (when data
      (let [view (.getMappedRange buffer)
            arr (js/Float32Array. view)]
        (.set arr (js/Float32Array. data))
        (.unmap buffer)))
    buffer))

(defn init-context! []
  (p/let [adapter (.requestAdapter js/navigator.gpu (clj->js {:powerPreference "high-performance"}))
          device (.requestDevice adapter)
          queue (.-queue device)
          module (.createShaderModule device #js {:code shader-source})
          pipeline (.createComputePipeline device
                                           #js {:layout "auto"
                                                :compute #js {:module module
                                                              :entryPoint "main"}})
          input-buffer (create-buffer device (bit-or (.-STORAGE js/GPUBufferUsage)
                                                     (.-COPY_DST js/GPUBufferUsage)) 4 nil)
          state-buffer (create-buffer device (bit-or (.-STORAGE js/GPUBufferUsage)
                                                     (.-COPY_DST js/GPUBufferUsage)) 8 #js [0 0])
          output-buffer (create-buffer device (bit-or (.-STORAGE js/GPUBufferUsage)
                                                      (.-COPY_SRC js/GPUBufferUsage)
                                                      (.-COPY_DST js/GPUBufferUsage)) 4 #js [0])
          params-buffer (create-buffer device (bit-or (.-UNIFORM js/GPUBufferUsage)
                                                      (.-COPY_DST js/GPUBufferUsage)) 16 nil)
          readback-buffer (.createBuffer device #js {:size 4
                                                     :usage (bit-or (.-COPY_DST js/GPUBufferUsage)
                                                                     (.-MAP_READ js/GPUBufferUsage))})
          bind-group (.createBindGroup device
                                       #js {:layout (.getBindGroupLayout pipeline 0)
                                            :entries (clj->js
                                                      [{:binding 0 :resource #js {:buffer input-buffer}}
                                                       {:binding 1 :resource #js {:buffer state-buffer}}
                                                       {:binding 2 :resource #js {:buffer output-buffer}}
                                                       {:binding 3 :resource #js {:buffer params-buffer}}]})}]
    {:device device
     :queue queue
     :pipeline pipeline
     :input-buffer input-buffer
     :state-buffer state-buffer
     :output-buffer output-buffer
     :params-buffer params-buffer
     :readback-buffer readback-buffer
     :bind-group bind-group}))

(defn dispatch!
  [{:keys [device queue pipeline bind-group input-buffer params-buffer output-buffer readback-buffer]} luminance {:keys [alpha low high dt]}]
  (let [input-arr (js/Float32Array. #js [luminance])
        params-arr (js/Float32Array. #js [alpha low high dt])
        command-encoder (.createCommandEncoder device)
        pass-encoder (.beginComputePass command-encoder)]
    (.writeBuffer queue input-buffer 0 input-arr)
    (.writeBuffer queue params-buffer 0 params-arr)
    (.setPipeline pass-encoder pipeline)
    (.setBindGroup pass-encoder 0 bind-group)
    (.dispatchWorkgroups pass-encoder 1)
    (.end pass-encoder)
    (.copyBufferToBuffer command-encoder output-buffer 0 readback-buffer 0 4)
    (.submit queue (clj->js [(.finish command-encoder)]))
    (p/let [_ (.onSubmittedWorkDone queue)
            _ (.mapAsync readback-buffer (.-READ js/GPUMapMode))]
      (let [mapped (.getMappedRange readback-buffer)
            arr (js/Float32Array. mapped)
            value (aget arr 0)]
        (.unmap readback-buffer)
        value))))

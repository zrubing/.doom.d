# go debug config example
.dir-locals.el

```elisp
(
 (nil . ((eval . (add-to-list 'dape-configs
             '(go-dlv-launch
               modes (go-mode go-ts-mode)
               command "dlv"
               command-args ("dap" "--listen" "127.0.0.1::autoport")
               port :autoport
               :type "go"
               :request "launch"
               :mode "debug"
               :program "./cmd/server"
               :cwd dape-cwd
               :env (:GOPROXY "xxx"
                     :GONOSUMDB "*"
                     :GODEBUG "madvdontneed=1")
               :name "Go Launch")))))
)
```

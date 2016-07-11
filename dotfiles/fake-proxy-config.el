;; Proxy

(setq url-proxy-services
   '(("no_proxy" . "^//(localhost|127.\.*|myserver.com)/")
     ("http" . "myproxy.com:port")
     ("https" . "myproxy.com:port")))

(provide 'proxy-config)

;; proxy-config.el ends here

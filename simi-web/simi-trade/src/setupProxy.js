const proxy=require("http-proxy-middleware")
module.exports=function(app){
    app.use(
        proxy.createProxyMiddleware("/simi-trade",{
            target: "http://127.0.0.1:48120",
            secure: false,
            changeOrigin: true,
            ws:true,
            pathRewrite: (path, req) =>path.replace('/simi-trade', '/')
        })
    )
}
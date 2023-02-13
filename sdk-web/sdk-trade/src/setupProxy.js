const proxy=require("http-proxy-middleware")

module.exports=function(app){
    app.use(
        proxy.createProxyMiddleware("/sdk-trade",{
            target: "http://localhost:48124",
            secure: false,
            changeOrigin: true,
            ws:true,
            pathRewrite:(path,req)=>{
                return path.replace("/sdk-trade","/")
            }
        })
    )
}
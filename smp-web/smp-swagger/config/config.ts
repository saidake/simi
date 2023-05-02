import {defineConfig} from 'umi';
import pageRoutes from "./router.config";

export default defineConfig({
    routes:pageRoutes,
    publicPath: '/',
    // mfsu:{},
    // webpack5:{},
    // outputPath:"..\\sdk-custom\\sdk-swagger-ui\\src\\main\\resources",
    outputPath:"dist",
    hash: false,
    history: {
      type: 'hash',
    },
    proxy: {                                     
      '/swagger': {                                                    
      'target': 'http://localhost:48999', 
     'changeOrigin': true,
    },
    '/api': {                                                    
      'target': 'http://localhost:48999', 
     'changeOrigin': true,
    },
  },

  })
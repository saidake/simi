export default [
    {
      path: '/',
      //component: '@/pages/Login/index'
      component: '../layouts/BaseLayout.tsx',
      routes: [
        { path: '/', component: '@/pages/Login' },
        {path:'/main', component: '@/pages/Main'}
      ]
    }
  ];
  
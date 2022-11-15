import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import {RecoilRoot} from 'recoil';
import reportWebVitals from "./reportWebVitals"
import 'normalize.css/normalize.css'
import "antd/dist/antd.min.css"
import './index.css';

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement
);

root.render(
  <React.StrictMode>
    <RecoilRoot>
        <App />
    </RecoilRoot>
  </React.StrictMode>
)

reportWebVitals();


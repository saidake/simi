import axios from 'axios';

export const fetchAccuntDataSync = async ()=>{
     const res=await axios.get("/sdk-trade/qihuo/account");
     return res.data;
}
import axios from 'axios';

export const fetchAccuntDataSync = async ()=>{
     const res=await axios.get("/sdk-citi/qihuo/account");
     return res.data;
}
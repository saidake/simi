import axios from 'axios';

export const fetchAccuntDataSync = async ()=>{
     const res=await axios.get("/sdk-citi/account");
     return res.data;
}
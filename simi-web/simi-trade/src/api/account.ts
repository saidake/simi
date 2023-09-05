import axios from 'axios';

export const fetchAccuntDataSync = async ()=>{
     const res=await axios.get("/simi-trade/fxcm/account");
     return res.data;
}
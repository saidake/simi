const moment=require( "moment")
console.log("test")
var day = moment("2022/1/14 22:05","YYYY/M/D H:mm",true)     
// /\d{4}/\d{1,2}/\d{1,2}/
const reg=/\d{4}\/\d{1,2}\/\d{1,2}/
const result=reg.exec("2022/1/14 22:05");
console.dir(!!result?result[0]:null)
let arrr=[3,5,6,7,3,2,-1,-1,2]


console.log(arrr.sort((a,b)=>a-b))
console.log(arrr)
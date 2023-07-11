import request from '@/utils/request.js';

export async function getSwaggerResourceList() {
  return request('/swagger-resources')
}


// export async function getApiDocs(params) {
//   return request('/v2/api-docs', params)
// }

export async function getApiDocs(url) {
    return request(url)
  }
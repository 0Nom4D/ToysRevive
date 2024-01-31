package nom4d.toysrevive.api.authentication

import okhttp3.Interceptor
import okhttp3.Response

import javax.inject.Inject

class AuthInterceptor @Inject constructor(
    private val tokenManager: TokenManager
) : Interceptor {
    override fun intercept(chain: Interceptor.Chain): Response {
        val currentRequest = chain.request()

        if (currentRequest.url.toString().let {
                it.endsWith("/register") || it.endsWith("/login")
            }
        ) {
            return chain.proceed(currentRequest)
        }
        val authenticatedRequest = currentRequest.newBuilder()
            .addHeader("Authorization", "Bearer ${tokenManager.getToken()}")
            .build()
        return chain.proceed(authenticatedRequest)
    }
}

package nom4d.toysrevive.api.authentication

import com.auth0.android.jwt.JWT
import dagger.hilt.android.scopes.ActivityScoped
import java.util.Date

@ActivityScoped
class TokenManager {
    private var jwtToken: String = ""
    private lateinit var jwtPayload: JWT

    fun getJWTPayload(): JWT = jwtPayload

    fun isPayloadExpired(): Boolean? = jwtPayload.expiresAt?.before(Date())

    fun getToken(): String = jwtToken

    fun updateJwtToken(token: String) {
        jwtToken = token
        jwtPayload = JWT(token)
    }
}

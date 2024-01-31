package nom4d.toysrevive.api.authentication

import com.auth0.android.jwt.JWT
import java.util.Date

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

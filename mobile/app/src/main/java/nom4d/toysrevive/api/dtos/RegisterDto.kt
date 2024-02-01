package nom4d.toysrevive.api.dtos

data class RegisterDto(
    val lastName: String,
    val firstName: String,
    val email: String,
    val userName: String,
    val phone: String,
    val password: String
)

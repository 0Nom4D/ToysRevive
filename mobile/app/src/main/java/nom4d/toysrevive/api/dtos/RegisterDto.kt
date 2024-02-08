package nom4d.toysrevive.api.dtos

data class RegisterDto(
    var lastName: String,
    var firstName: String,
    var email: String,
    var userName: String,
    var phone: String,
    var password: String
)

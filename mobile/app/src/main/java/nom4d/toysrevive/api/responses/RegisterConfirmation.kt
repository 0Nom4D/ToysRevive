package nom4d.toysrevive.api.responses

data class RegisterConfirmation(
    val id: Int,
    val lastName: String,
    val firstName: String,
    val userName: String,
    val phone: String,
    val email: String
)

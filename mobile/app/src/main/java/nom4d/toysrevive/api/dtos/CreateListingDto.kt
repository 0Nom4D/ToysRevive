package nom4d.toysrevive.api.dtos

data class CreateListingDto(
    val title: String,
    val description: String,
    val type: String,
    val condition: String,
    val postCode: Int
)

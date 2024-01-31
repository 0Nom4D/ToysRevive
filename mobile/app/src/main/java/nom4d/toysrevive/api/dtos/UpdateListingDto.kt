package nom4d.toysrevive.api.dtos

data class UpdateListingDto(
    val title: String?,
    val type: String?,
    val description: String?,
    val condition: String?,
    val postCode: Int?
)

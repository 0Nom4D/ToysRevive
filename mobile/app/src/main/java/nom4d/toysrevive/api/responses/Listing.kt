package nom4d.toysrevive.api.responses

data class Listing(
    val addDate: String,
    val condition: String,
    val description: String,
    val id: Int,
    val images: List<Image>?,
    val ownerId: Int,
    val postCode: Int,
    val title: String,
    val type: String
)

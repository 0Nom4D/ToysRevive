package nom4d.toysrevive.api.responses

data class ManyResults<T>(
    val items: List<T>,
    val metadata: Metadata
)

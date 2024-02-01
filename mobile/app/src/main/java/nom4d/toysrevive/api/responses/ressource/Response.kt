package nom4d.toysrevive.api.responses.ressource

sealed class Response<T>(val type: ResponseType, val data: T? = null, val message: String? = null) {
    class Success<T>(data: T) : Response<T>(ResponseType.SUCCESS, data)
    class Error<T>(
        message: String,
        data: T? = null
    ) : Response<T>(ResponseType.ERROR, data, message)
    class Loading<T>(data: T? = null) : Response<T>(ResponseType.LOADING, data)
}

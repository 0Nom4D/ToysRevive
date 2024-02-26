package nom4d.toysrevive.api

import nom4d.toysrevive.api.dtos.CreateListingDto
import nom4d.toysrevive.api.dtos.LoginDto
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.api.dtos.UpdateListingDto
import nom4d.toysrevive.api.responses.AccessToken
import nom4d.toysrevive.api.responses.Image
import nom4d.toysrevive.api.responses.Listing
import nom4d.toysrevive.api.responses.ManyResults
import nom4d.toysrevive.api.responses.RegisterConfirmation
import okhttp3.MultipartBody
import retrofit2.http.Body
import retrofit2.http.DELETE
import retrofit2.http.GET
import retrofit2.http.Multipart
import retrofit2.http.POST
import retrofit2.http.PUT
import retrofit2.http.Part
import retrofit2.http.Path
import retrofit2.http.Query

interface ToysReviveApi {
    // Authentification Routes
    @POST("/auth/login")
    suspend fun loginUser(@Body loginDto: LoginDto): AccessToken

    @POST("/auth/register")
    suspend fun registerUser(@Body registerDto: RegisterDto): RegisterConfirmation

    // Listings Routes
    @GET("/listings")
    suspend fun getListings(
        @Query("take") take: Int? = null,
        @Query("postCode") postCode: String? = null,
        @Query("condition") condition: String? = null,
        @Query("type") type: String? = null,
        @Query("liked") liked: Boolean? = null
    ): ManyResults<Listing>

    @POST("/listings")
    suspend fun postListing(@Body listingDto: CreateListingDto): Listing

    @PUT("/listings/{id}/like")
    suspend fun likeListing(@Path("id") id: Int)

    @PUT("/listings/{id}/dislike")
    suspend fun dislikeListing(@Path("id") id: Int)

    @PUT("/listings/{id}")
    suspend fun updateListing(
        @Path("id") id: Int,
        @Body updateListingDto: UpdateListingDto
    ): Listing

    @DELETE("/listings/{id}")
    suspend fun deleteListing(@Path("id") id: Int): Listing

    // Images Routes
    @Multipart
    @POST("/images/{listingId}")
    suspend fun postImage(@Path("listingId") listingId: Int, @Part file: MultipartBody.Part): Image

    @DELETE("/images/{id}")
    suspend fun deleteImage(@Path("id") imageId: Int): Image
}

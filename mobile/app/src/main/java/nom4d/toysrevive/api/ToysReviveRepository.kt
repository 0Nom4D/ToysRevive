package nom4d.toysrevive.api

import android.net.Uri
import androidx.core.net.toFile
import dagger.hilt.android.scopes.ActivityScoped
import nom4d.toysrevive.api.dtos.CreateListingDto
import nom4d.toysrevive.api.dtos.LoginDto
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.api.dtos.UpdateListingDto
import nom4d.toysrevive.api.responses.AccessToken
import nom4d.toysrevive.api.responses.Image
import nom4d.toysrevive.api.responses.Listing
import nom4d.toysrevive.api.responses.ManyResults
import nom4d.toysrevive.api.responses.RegisterConfirmation
import nom4d.toysrevive.api.responses.ressource.Response
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.MultipartBody
import okhttp3.RequestBody.Companion.asRequestBody
import javax.inject.Inject

@ActivityScoped
class ToysReviveRepository @Inject constructor(
    private val api: ToysReviveApi
) {
    private val unknownErrorMessage: String = "An unknown error has occurred."

    suspend fun login(loginDto: LoginDto): Response<AccessToken> {
        val accessToken = try {
            api.loginUser(loginDto)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(accessToken)
    }

    suspend fun register(registerDto: RegisterDto): Response<RegisterConfirmation> {
        val registration = try {
            api.registerUser(registerDto)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(registration)
    }

    suspend fun getListings(
        take: Int? = null,
        postCode: String? = null,
        condition: String? = null,
        type: String? = null
    ): Response<ManyResults<Listing>> {
        val response = try {
            api.getListings(take, postCode, condition, type)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(response)
    }

    suspend fun createListing(listingDto: CreateListingDto): Response<Listing> {
        val response = try {
            api.postListing(listingDto)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(response)
    }

    suspend fun updateListing(
        listingId: Int,
        updateListingDto: UpdateListingDto
    ): Response<Listing> {
        val response = try {
            api.updateListing(listingId, updateListingDto)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(response)
    }

    suspend fun deleteListing(listingId: Int): Response<Listing> {
        val response = try {
            api.deleteListing(listingId)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(response)
    }

    suspend fun postImage(listingId: Int, fileUri: Uri): Response<Image> = try {
        val file = fileUri.toFile()
        val multipartBody = MultipartBody.Part.createFormData(
            "file",
            file.name,
            file.asRequestBody("image/*".toMediaTypeOrNull())
        )
        Response.Success(api.postImage(listingId, multipartBody))
    } catch (e: retrofit2.HttpException) {
        Response.Error(e.message())
    } catch (_: Exception) {
        Response.Error(unknownErrorMessage)
    }

    suspend fun deleteImage(imageId: Int): Response<Image> {
        val response = try {
            api.deleteImage(imageId)
        } catch (e: retrofit2.HttpException) {
            return Response.Error(e.message())
        } catch (_: Exception) {
            return Response.Error(unknownErrorMessage)
        }
        return Response.Success(response)
    }
}

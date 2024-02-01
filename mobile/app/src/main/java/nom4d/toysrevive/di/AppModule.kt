package nom4d.toysrevive.di

import dagger.Module
import dagger.Provides
import dagger.hilt.InstallIn
import dagger.hilt.components.SingletonComponent
import nom4d.toysrevive.BuildConfig
import nom4d.toysrevive.api.ToysReviveApi
import nom4d.toysrevive.api.ToysReviveRepository
import nom4d.toysrevive.api.authentication.AuthInterceptor
import nom4d.toysrevive.api.authentication.TokenManager
import okhttp3.OkHttpClient
import okhttp3.logging.HttpLoggingInterceptor
import retrofit2.Retrofit
import retrofit2.converter.moshi.MoshiConverterFactory
import javax.inject.Singleton

@Module
@InstallIn(SingletonComponent::class)
object AppModule {

    @Singleton
    @Provides
    fun providesToysReviveApiWrapper(api: ToysReviveApi) = ToysReviveRepository(api)

    @Singleton
    @Provides
    fun providesApiLauncher(manager: TokenManager): ToysReviveApi {
        return Retrofit.Builder()
            .addConverterFactory(MoshiConverterFactory.create())
            .baseUrl(BuildConfig.API_URL)
            .client(
                OkHttpClient.Builder()
                    .addInterceptor(AuthInterceptor(manager))
                    .addInterceptor(
                        HttpLoggingInterceptor()
                            .apply {
                                this.level = HttpLoggingInterceptor.Level.BASIC
                            }
                    )
                    .build()
            )
            .build()
            .create(ToysReviveApi::class.java)
    }

    @Singleton
    @Provides
    fun provideTokenManager(): TokenManager {
        return TokenManager()
    }
}

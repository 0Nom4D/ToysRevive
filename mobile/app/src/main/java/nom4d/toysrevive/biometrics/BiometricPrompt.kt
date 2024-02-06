package nom4d.toysrevive.biometrics

import android.content.Context
import androidx.biometric.BiometricManager
import androidx.biometric.BiometricPrompt
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.SheetState
import androidx.compose.material3.SnackbarHostState
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.launch
import nom4d.toysrevive.ext.getActivity
import nom4d.toysrevive.settings.AppSettings

@OptIn(ExperimentalMaterial3Api::class)
fun openSettingsBottomModalSheet(
    context: Context,
    coroutineScope: CoroutineScope,
    scaffoldState: SnackbarHostState,
    bottomSheetState: SheetState
) {
    AppSettings.isToyDescription.value = false
    if (AppSettings.canBeAuthenticateWithFinderPrint && AppSettings.isFingerprintProtected) {
        showBiometricPrompt(
            context,
            coroutineScope,
            scaffoldState,
            bottomSheetState
        )
    } else {
        coroutineScope.launch {
            bottomSheetState.show()
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
private fun showBiometricPrompt(
    context: Context,
    coroutineScope: CoroutineScope,
    scaffoldState: SnackbarHostState,
    bottomSheetState: SheetState
) {
    val promptInfo = BiometricPrompt.PromptInfo.Builder()
        .setTitle("Authenticate to get to settings")
        .setSubtitle(
            "This settings section is protected by biometric prompt." +
                "Please authenticate to access settings."
        )
        .setNegativeButtonText("Cancel")
        .setConfirmationRequired(true)
        .setAllowedAuthenticators(BiometricManager.Authenticators.BIOMETRIC_STRONG)
        .build()

    context.getActivity().let {
        val biometricPrompt = it?.let { it1 ->
            BiometricPrompt(
                it1,
                object : BiometricPrompt.AuthenticationCallback() {
                    override fun onAuthenticationError(errorCode: Int, errString: CharSequence) {
                        super.onAuthenticationError(errorCode, errString)
                        coroutineScope.launch {
                            scaffoldState.showSnackbar(
                                "Authentication failed.",
                                ""
                            )
                        }
                    }

                    override fun onAuthenticationSucceeded(
                        result: BiometricPrompt.AuthenticationResult
                    ) {
                        super.onAuthenticationSucceeded(result)
                        coroutineScope.launch {
                            bottomSheetState.show()
                        }
                    }

                    override fun onAuthenticationFailed() {
                        super.onAuthenticationFailed()
                        coroutineScope.launch {
                            scaffoldState.showSnackbar(
                                "Authentication failed.",
                                ""
                            )
                        }
                    }
                }
            )
        }

        biometricPrompt?.authenticate(promptInfo)
    }
}

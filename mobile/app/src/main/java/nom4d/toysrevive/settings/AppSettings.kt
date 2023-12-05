package nom4d.toysrevive.settings

import androidx.compose.runtime.mutableStateOf

object AppSettings {
    var canBeAuthenticateWithFinderPrint: Boolean = false
    var isFingerprintProtected: Boolean = false

    var isToyDescription = mutableStateOf(false)
}

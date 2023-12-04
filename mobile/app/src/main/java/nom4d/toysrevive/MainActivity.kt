package nom4d.toysrevive

import android.os.Bundle
import androidx.activity.compose.setContent
import androidx.appcompat.app.AppCompatActivity

import androidx.biometric.BiometricManager
import androidx.biometric.BiometricManager.Authenticators.BIOMETRIC_STRONG
import androidx.biometric.BiometricPrompt

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape

import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material3.BottomSheetScaffold
import androidx.compose.material3.Card
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SheetState
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.rememberBottomSheetScaffoldState
import androidx.compose.material3.rememberModalBottomSheetState

import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope

import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.launch

import nom4d.toysrevive.navigation.BottomNavigationBar
import nom4d.toysrevive.navigation.NavigationHost
import nom4d.toysrevive.navigation.NavigationItem
import nom4d.toysrevive.navigation.getRouteDisplayName
import nom4d.toysrevive.ui.theme.ToysReviveTheme

@OptIn(ExperimentalMaterial3Api::class)
class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            ToysReviveTheme {
                val navController = rememberNavController()
                val navBackStackEntry by navController.currentBackStackEntryAsState()
                val currentRoute = navBackStackEntry?.destination?.route

                val scaffoldState = remember { SnackbarHostState() }
                val coroutineScope = rememberCoroutineScope()

                val settingsModalState = rememberModalBottomSheetState(
                    skipPartiallyExpanded = true
                )
                val scaffoldModalState = rememberBottomSheetScaffoldState(
                    bottomSheetState = settingsModalState,
                    snackbarHostState = scaffoldState
                )

                val biometricManager = BiometricManager.from(this)
                when (biometricManager.canAuthenticate(BIOMETRIC_STRONG)) {
                    BiometricManager.BIOMETRIC_SUCCESS -> print("ALED")
                    else -> {}
                }

                BottomSheetScaffold(
                    scaffoldState = scaffoldModalState,
                    sheetContent = {}
                ) {
                    Scaffold(
                        topBar = {
                            TopAppBar(
                                title = {
                                    getRouteDisplayName(currentRoute)?.let {
                                        Text(it)
                                    }
                                },
                                actions = {
                                    if (currentRoute == NavigationItem.HomeScreen.routeName) {
                                        Card(
                                            shape = RoundedCornerShape(10.dp)
                                        ) {
                                            IconButton(
                                                onClick = {
                                                    showBiometricPrompt(
                                                        coroutineScope,
                                                        scaffoldState,
                                                        scaffoldModalState.bottomSheetState
                                                    )
                                                }
                                            ) {
                                                Icon(
                                                    Icons.Filled.Settings,
                                                    contentDescription = "Swipe settings"
                                                )
                                            }
                                        }
                                    }
                                }
                            )
                        },
                        snackbarHost = { SnackbarHost(scaffoldState) },
                        bottomBar = {
                            BottomNavigationBar(
                                navController = navController
                            )
                        },
                        modifier = Modifier.fillMaxSize()
                    ) {
                        NavigationHost(scaffoldState, navController, Modifier.padding(it))
                    }
                }
            }
        }
    }

    private fun showBiometricPrompt(
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
            .setAllowedAuthenticators(BIOMETRIC_STRONG)
            .build()

        val biometricPrompt = BiometricPrompt(
            this@MainActivity,
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

        biometricPrompt.authenticate(promptInfo)
    }
}

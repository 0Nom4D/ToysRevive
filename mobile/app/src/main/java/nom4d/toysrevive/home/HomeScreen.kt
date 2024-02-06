package nom4d.toysrevive.home

import android.os.Build

import androidx.annotation.RequiresApi
import androidx.biometric.BiometricManager

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
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.rememberBottomSheetScaffoldState
import androidx.compose.material3.rememberModalBottomSheetState

import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope

import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp

import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController

import nom4d.toysrevive.biometrics.openSettingsBottomModalSheet
import nom4d.toysrevive.ext.getActivity
import nom4d.toysrevive.navigation.BottomNavigationBar
import nom4d.toysrevive.navigation.MainNavigationHost
import nom4d.toysrevive.navigation.NavigationItem
import nom4d.toysrevive.navigation.getRouteDisplayName
import nom4d.toysrevive.settings.AppSettings
import nom4d.toysrevive.settings.SettingsModal
import nom4d.toysrevive.swipe.SwipeableCardDescriptionModal

@RequiresApi(Build.VERSION_CODES.R)
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun HomeScreen() {
    val context = LocalContext.current
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

    val biometricManager = context.getActivity()?.let { BiometricManager.from(it) }
    if (biometricManager != null) {
        when (biometricManager.canAuthenticate(BiometricManager.Authenticators.BIOMETRIC_STRONG)) {
            BiometricManager.BIOMETRIC_SUCCESS ->
                AppSettings.canBeAuthenticateWithFinderPrint = true

            else -> AppSettings.canBeAuthenticateWithFinderPrint = false
        }
    }

    BottomSheetScaffold(
        scaffoldState = scaffoldModalState,
        sheetContent = {
            if (!AppSettings.isToyDescription.value) {
                SettingsModal()
            } else {
                SwipeableCardDescriptionModal()
            }
        }
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
                                        openSettingsBottomModalSheet(
                                            context,
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
            MainNavigationHost(
                scaffoldState,
                navController,
                settingsModalState,
                Modifier.padding(it)
            )
        }
    }
}

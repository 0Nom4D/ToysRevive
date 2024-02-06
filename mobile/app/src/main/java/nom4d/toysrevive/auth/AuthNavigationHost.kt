package nom4d.toysrevive.auth

import android.os.Build
import androidx.annotation.RequiresApi
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import nom4d.toysrevive.home.HomeScreen
import nom4d.toysrevive.navigation.NavigationItem

@RequiresApi(Build.VERSION_CODES.R)
@Composable
fun AuthNavigationHost(modifier: Modifier = Modifier) {
    val navHostController = rememberNavController()

    NavHost(
        navController = navHostController,
        startDestination = NavigationItem.LoginScreen.routeName
    ) {
        composable(route = NavigationItem.LoginScreen.routeName) {
            LoginView(navHostController, modifier = modifier)
        }
        composable(route = "register") {
            // Register View
        }
        composable(route = "home") {
            HomeScreen()
        }
    }
}

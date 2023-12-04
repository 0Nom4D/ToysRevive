package nom4d.toysrevive.navigation

import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Text

import androidx.compose.runtime.Composable

import androidx.compose.ui.Modifier

import androidx.navigation.NavHostController
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable

@Composable
fun NavigationHost(
    scaffoldState: SnackbarHostState,
    navHostController: NavHostController,
    modifier: Modifier = Modifier
) {
    NavHost(
        navController = navHostController,
        startDestination = NavigationItem.HomeScreen.routeName
    ) {
        composable(route = NavigationItem.HomeScreen.routeName) {
            Text("Home Screen")
        }
        composable(route = NavigationItem.ToyAdCreationScreen.routeName) {
            Text("Toy Ad Creation Screen")
        }
        composable(route = NavigationItem.WishListScreen.routeName) {
            Text("Wish List Screen")
        }
    }
}

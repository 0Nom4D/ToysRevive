package nom4d.toysrevive.navigation

import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Favorite
import androidx.compose.material.icons.filled.Person
import androidx.compose.material3.BottomAppBar
import androidx.compose.material3.Icon
import androidx.compose.material3.NavigationBarItem
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.vectorResource
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import androidx.navigation.compose.currentBackStackEntryAsState
import nom4d.toysrevive.R

@Composable
fun BottomNavigationBar(navController: NavController, modifier: Modifier = Modifier) {
    val navigationItems = listOf(
        NavigationItem.WishListScreen,
        NavigationItem.HomeScreen,
        NavigationItem.ToyAdCreationScreen,
        NavigationItem.ProfileScreen
    )

    val navigationItemIcons = listOf(
        Icons.Filled.Favorite,
        ImageVector.vectorResource(id = R.drawable.baseline_toys_24),
        Icons.Filled.Add,
        Icons.Filled.Person
    )

    BottomAppBar(
        tonalElevation = 5.dp,
        modifier = modifier
    ) {
        val navBackStackEntry by navController.currentBackStackEntryAsState()
        val currentRoute = navBackStackEntry?.destination?.route

        navigationItems.forEachIndexed { index, item ->
            NavigationBarItem(
                icon = { Icon(navigationItemIcons[index], contentDescription = "") },
                alwaysShowLabel = false,
                selected = currentRoute == item.routeName,
                onClick = {
                    navController.navigate(item.routeName) {
                        navController.graph.startDestinationRoute?.let { route ->
                            popUpTo(route) {
                                saveState = true
                            }
                        }
                        launchSingleTop = true
                        restoreState = true
                    }
                }
            )
        }
    }
}

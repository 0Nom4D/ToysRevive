package nom4d.toysrevive.navigation

sealed class NavigationItem(
    val routeName: String,
    val appBarDisplayName: String
) {
    data object LoginScreen : NavigationItem("login", "")
    data object WishListScreen : NavigationItem("wish", "Liked Toys")
    data object HomeScreen : NavigationItem("home", "")
    data object ToyAdCreationScreen : NavigationItem("create", "Give a toy")
    data object ProfileScreen : NavigationItem("me", "Profile")
}

fun getRouteDisplayName(routeName: String?): String? {
    val availableRoutes = listOf(
        NavigationItem.LoginScreen,
        NavigationItem.HomeScreen,
        NavigationItem.WishListScreen,
        NavigationItem.ToyAdCreationScreen,
        NavigationItem.ProfileScreen
    )

    if (routeName == null) {
        return null
    }

    availableRoutes.forEach {
        if (it.routeName == routeName) {
            return it.appBarDisplayName
        }
    }
    return null
}

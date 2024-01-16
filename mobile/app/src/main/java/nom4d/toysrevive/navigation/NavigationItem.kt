package nom4d.toysrevive.navigation

sealed class NavigationItem(
    val routeName: String,
    val appBarDisplayName: String
) {
    data object WishListScreen : NavigationItem("Wish List", "Liked Toys")
    data object HomeScreen : NavigationItem("Home", "")
    data object ToyAdCreationScreen : NavigationItem("Ad Creation", "Give a toy")
    data object ProfileScreen : NavigationItem("Profile", "Profile")
}

fun getRouteDisplayName(routeName: String?): String? {
    val availableRoutes = listOf(
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

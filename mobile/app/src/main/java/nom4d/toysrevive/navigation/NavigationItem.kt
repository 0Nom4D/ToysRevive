package nom4d.toysrevive.navigation

sealed class NavigationItem(
    val screenName: String,
    val routeName: String,
    val appBarDisplayName: String
) {
    data object WishListScreen : NavigationItem("Saved Toys", "Wish List", "Liked Toys")
    data object HomeScreen : NavigationItem("Swipe Screen", "Home", "")
    data object ToyAdCreationScreen : NavigationItem("Give a toy", "Ad Creation", "Give a toy")
}

fun getRouteDisplayName(routeName: String?): String? {
    val availableRoutes = listOf(
        NavigationItem.HomeScreen,
        NavigationItem.WishListScreen,
        NavigationItem.ToyAdCreationScreen
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

package nom4d.toysrevive.navigation

import android.os.Build
import android.util.Log
import androidx.annotation.RequiresApi
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.SheetState
import androidx.compose.material3.SnackbarHostState
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.navigation.NavHostController
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import coil.compose.rememberAsyncImagePainter
import com.alexstyl.swipeablecard.rememberSwipeableCardState
import nom4d.toysrevive.swipe.SwipeView
import nom4d.toysrevive.swipe.SwipeableImageCard

@OptIn(ExperimentalMaterial3Api::class)
@RequiresApi(Build.VERSION_CODES.R)
@Composable
fun NavigationHost(
    scaffoldState: SnackbarHostState,
    navHostController: NavHostController,
    settingsModalState: SheetState,
    modifier: Modifier = Modifier
) {
    val painter = rememberAsyncImagePainter("https://pbs.twimg.com/media/Ex0MTkpXMAIE87u.jpg")
    val state = rememberSwipeableCardState()

    NavHost(
        navController = navHostController,
        startDestination = NavigationItem.HomeScreen.routeName
    ) {
        composable(route = NavigationItem.HomeScreen.routeName) {
            SwipeView(settingsModalState, modifier)
        }
        composable(route = NavigationItem.ToyAdCreationScreen.routeName) {
            SwipeableImageCard(
                painter = painter,
                contentDescription = "KFC Espagne Image",
                imageTitle = "test ad",
                state = state,
                onDetailButtonClicked = { Log.d("TEST CREATE SCREEN", "Test for Swipeable on Toy Ad Creation screen") },
            )
        }
        composable(route = NavigationItem.WishListScreen.routeName) {
            SwipeableImageCard(
                painter = painter,
                contentDescription = "KFC Espagne Image",
                imageTitle = "test love",
                state = state,
                onDetailButtonClicked = { Log.d("TEST WISH SCREEN", "Test for Swipeable on Love screen") },
            )
        }
        composable(route = NavigationItem.ProfileScreen.routeName) {
            SwipeableImageCard(
                painter = painter,
                contentDescription = "KFC Espagne Image",
                imageTitle = "test love",
                state = state,
                onDetailButtonClicked = { Log.d("TEST PROFILE SCREEN", "Test for Swipeable on Profile screen") },
            )
        }
    }
}

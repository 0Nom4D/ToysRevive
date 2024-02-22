package nom4d.toysrevive.auth

import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.wrapContentHeight
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.navigation.NavHostController
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.auth.steps.IdsStep
import nom4d.toysrevive.auth.steps.NameStep
import nom4d.toysrevive.auth.steps.PasswordStep
import nom4d.toysrevive.auth.steps.PhoneStep

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun RegisterView(navHostController: NavHostController, modifier: Modifier = Modifier) {
    var registerDto by remember { mutableStateOf(RegisterDto()) }
    val pagerState = rememberPagerState(pageCount = { 4 })

    Column(modifier = modifier.fillMaxSize()) {
        HorizontalPager(
            state = pagerState,
            userScrollEnabled = false,
            modifier = Modifier
                .fillMaxHeight(.95f)
                .fillMaxWidth()
        ) {
            when (it) {
                0 -> NameStep(
                    navHostController = navHostController,
                    dto = registerDto,
                    pagerState = pagerState,
                    modifier = Modifier.align(Alignment.CenterHorizontally)
                )
                1 -> IdsStep(
                    dto = registerDto,
                    pagerState = pagerState,
                    modifier = Modifier.align(Alignment.CenterHorizontally)
                )
                2 -> PhoneStep(
                    dto = registerDto,
                    pagerState = pagerState,
                    modifier = Modifier.align(Alignment.CenterHorizontally)
                )
                3 -> PasswordStep(
                    dto = registerDto,
                    pagerState = pagerState,
                    modifier = Modifier.align(Alignment.CenterHorizontally),
                    navHostController = navHostController
                )
            }
        }
        Row(
            Modifier
                .wrapContentHeight()
                .fillMaxWidth()
                .align(Alignment.CenterHorizontally)
                .padding(bottom = 8.dp),
            horizontalArrangement = Arrangement.Center
        ) {
            repeat(pagerState.pageCount) { iteration ->
                val color =
                    if (pagerState.currentPage == iteration) Color.DarkGray else Color.LightGray
                Box(
                    modifier = Modifier
                        .padding(2.dp)
                        .clip(CircleShape)
                        .background(color)
                        .size(16.dp)
                )
            }
        }
    }
}

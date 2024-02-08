package nom4d.toysrevive.auth

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.lifecycle.ViewModel
import androidx.lifecycle.viewModelScope
import dagger.hilt.android.lifecycle.HiltViewModel
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import nom4d.toysrevive.api.ToysReviveRepository
import nom4d.toysrevive.api.dtos.RegisterDto
import nom4d.toysrevive.api.responses.ressource.Response
import javax.inject.Inject

@HiltViewModel
class RegisterViewModel @Inject constructor(
    private val repository: ToysReviveRepository
) : ViewModel() {
    private var isLoading: MutableState<Boolean> = mutableStateOf(false)
    var errorMessage: MutableState<String> = mutableStateOf("")

    private val dispatcher: CoroutineDispatcher = Dispatchers.Default

    fun registerUser(registerDto: RegisterDto): Boolean {
        viewModelScope.launch(dispatcher) {
            isLoading.value = true
            when (val result = repository.register(registerDto)) {
                is Response.Success -> {
                    isLoading.value = false
                }
                is Response.Error -> {
                    errorMessage.value = result.message!!
                    isLoading.value = false
                }
                else -> {}
            }
        }
        return errorMessage.value.isEmpty()
    }
}

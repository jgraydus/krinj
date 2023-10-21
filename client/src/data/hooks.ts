import * as ReactRedux from 'react-redux'
import { store } from './store'

type DispatchFunc = () => typeof store.dispatch

export const useDispatch: DispatchFunc = ReactRedux.useDispatch
export const useSelector: ReactRedux.TypedUseSelectorHook<RootState> = ReactRedux.useSelector


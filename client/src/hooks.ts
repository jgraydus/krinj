import * as ReactRedux from 'react-redux'
import { RootState, store } from './redux'

type DispatchFunc = () => typeof store.dispatch

export const useDispatch: DispatchFunc = ReactRedux.useDispatch
export const useSelector: ReactRedux.TypedUseSelectorHook<RootState> = ReactRedux.useSelector


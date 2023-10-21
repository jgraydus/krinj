import * as ReactRedux from 'react-redux';
import { store } from './store';
type DispatchFunc = () => typeof store.dispatch;
export declare const useDispatch: DispatchFunc;
export declare const useSelector: ReactRedux.TypedUseSelectorHook<RootState>;
export {};

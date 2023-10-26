import type { ReactNode } from 'react'
import { useCallback, useState } from 'react'
import styled from 'styled-components'
import { Button } from 'components'
import { logOut, meSelector, showLogInView, useDispatch, useSelector } from 'data'

const Root = styled.div`
  height: 40px;
  width: 40px;
  position: relative;
`
const Handle = styled.div`
  cursor: pointer;
  height: 40px;
  width: 40px;
  border: 1px solid red;
`
const MenuRoot = styled.div`
  cursor: pointer;
  width: 100px;
  border: 1px solid red;
  background-color: white;
  position: absolute;
  right: 0;
  top: 40px;
  display: flex;
  flex-direction: column;
`
const Menu = ({ children }: { children: ReactNode }) => {
  return <MenuRoot>{children}</MenuRoot>
}
const MenuItem = styled(
  ({ children, className, onClick }: { children: ReactNode, className?: string, onClick?: () => void }) => {
  return (
    <div className={className} onClick={onClick}>
      {children}
    </div>
  );
})`
   height: 30px;
   width: 100%;
   display: flex;
   align-items: center;
   padding-left: 5px;
`
export default () => {
  const dispatch = useDispatch();
  const me = useSelector(meSelector);
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  const toggle = useCallback(() => {
    setIsMenuOpen(!isMenuOpen)
  }, [isMenuOpen]);

  const doLogOut = useCallback(() => {
    dispatch(logOut());
  }, []);

  if (!me) {
    return (
      <Button onClick={() => dispatch(showLogInView())}>Log In</Button>
    );
  }

  return (
      <Root>
         <Handle onClick={toggle} />
         {isMenuOpen && (
           <Menu>
             <MenuItem>Settings</MenuItem>
             <MenuItem onClick={doLogOut}>Log Out</MenuItem>
           </Menu>
         )}
      </Root>
  );
}


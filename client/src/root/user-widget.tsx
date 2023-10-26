import type { ReactNode } from 'react'
import { useCallback, useState } from 'react'
import styled from 'styled-components'
import { CarotIcon } from 'components'
import { logOut, meSelector, showLogInView, useDispatch, useSelector } from 'data'

const Root = styled.div`
  height: 30px;
  width: 30px;
  position: relative;
`
const LogInLink = styled.div`
  color: #506060;
  cursor: pointer;
  font-size: 16px;
  :hover { color: #809090; }
`
const Handle = styled.div`
  cursor: pointer;
  height: 30px;
  width: 30px;
  border: 1px solid black;
  border-radius: 4px;
  display: flex;
  align-items: center;
  justify-content: center;
`
const MenuRoot = styled.div`
  cursor: pointer;
  width: 120px;
  border: 1px solid black;
  border-radius: 4px;
  background-color: #B6C6C6;
  position: absolute;
  right: 0;
  top: 30px;
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
  border-radius: 4px;
  align-items: center;
  padding-left: 5px;
  background-color: #DFEFEF;
  :hover {
    background-color: #BFCFCF;
  }
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
      <LogInLink onClick={() => dispatch(showLogInView())}>log in</LogInLink>
    );
  }

  return (
      <Root>
         <Handle onClick={toggle}>
           <CarotIcon size={20} direction={isMenuOpen ? 'down' : 'up'} color="#506060"/>
         </Handle>
         {isMenuOpen && (
           <Menu>
             <MenuItem>Settings</MenuItem>
             <MenuItem onClick={doLogOut}>Log Out</MenuItem>
           </Menu>
         )}
      </Root>
  );
}


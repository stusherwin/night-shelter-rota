import * as React from 'react';
import { Vol } from './Types'

export interface HeaderProps { reqInProgress: boolean, errorMessage: string | null, vols: Vol[] }
export interface HeaderState {}

export class Header extends React.Component<HeaderProps, HeaderState> {
    render() {
      return <div className="header initial-data-loading">
               {this.statusIcon()}
               <h2>Night Shelter Rota</h2>
               {this.props.vols.map(v => <p>{v.name}</p>)}
             </div>
    }

    statusIcon() {
      return <div className="header-status">
               <i className={this.iconType()}></i>
             </div>
                                          // onClick={ e => e.preventDefault(); } $ R.preventDefault >=> (const $ dispatch $ MessageBubbleAction ToggleFixed)
                                          // , RP.onMouseOver $ const $ dispatch $ MessageBubbleAction ShowTransitory
                                          // , RP.onMouseLeave $ const $ dispatch $ MessageBubbleAction HideTransitory
    }

    iconType() {
      return this.props.reqInProgress
               ? 'icon-spin animate-spin'
               : 'logo'
    }
}

  // statusIcon dispatch s = [ RD.div [ RP.className 'header-status' ]
  //                                  $ 
  //                                  [ RD.i [ RP.className iconType
  //                                         , RP.onClick $ R.preventDefault >=> (const $ dispatch $ MessageBubbleAction ToggleFixed)
  //                                         , RP.onMouseOver $ const $ dispatch $ MessageBubbleAction ShowTransitory
  //                                         , RP.onMouseLeave $ const $ dispatch $ MessageBubbleAction HideTransitory
  //                                         ] 
  //                                         []
  //                                  ]
  //                                  <>
  //                                  renderMessageBubble (dispatch <<< MessageBubbleAction) s.errorMessage
  //                         ]
  //   where
  //   iconType = case s.reqInProgress, s.errorMessage of
  //                true,  _              -> 'icon-spin animate-spin'
  //                false, Hidden Nothing -> 'logo'
  //                _,     _              -> 'icon-warning'

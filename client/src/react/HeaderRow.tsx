import * as React from 'react';

export interface HeaderRowProps { showPrev: boolean
                                , showNext: boolean
                                , loadPrevPeriod: () => void
                                , loadNextPeriod: () => void
                                }

export class HeaderRow extends React.Component<HeaderRowProps, {}> {
  render() {
    return (
      <div className="row row-header">
        <div className="row-header-actions">
          {this.props.showPrev
            ? <a onClick={e => {e.preventDefault(); this.props.loadPrevPeriod()}}
                 href="#"
                 className="action">
                <i className="icon-up-open"></i>
                <span>previous 4 weeks</span>
              </a>
            : null}
          {this.props.showNext
            ? <a onClick={e => {e.preventDefault(); this.props.loadNextPeriod()}}
                 href="#"
                 className="action">
                <i className="icon-up-open"></i>
                <span>previous 4 weeks</span>
              </a>
            : null}
        </div>
        {this.props.children}
      </div>
    )
  }
}
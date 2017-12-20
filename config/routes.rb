# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

Verboice::Application.routes.draw do

  resources :channels do
    resources :queued_calls
    member do
      get :call
      post :enable
      post :disable
    end
  end

  resources :nuntium_channels, except: [:show]

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts, controllers: {omniauth_callbacks: "omniauth_callbacks"}
  guisso_for :account

  resources :feeds, controller: :feed_server do
    member do
      get :recordings
      get 'recording/:recording_id', action: :get_recording, as: :recording
    end
  end

  # Register both shallow and deep routes:
  # - Shallow routes allow for easier path helper methods, such as contact_recorded_audios(@contact) instead of project_contact_recorded_audios(@project, @contact)
  # - Deep routes ensure that form_for directives work as expected, so form_for([@project, @contact]) works no matter it is a creation or an update
  [true, false].each do |shallow|
    resources :projects, :shallow => shallow do
      member do
        post :enqueue_call
        put :update_variables
      end

      resources :call_flows, except: [:new, :edit] do
        member do
          get :edit_workflow, path: :designer
          put :update_workflow, path: :update_flow
          post :import
          get :export
          get :download_results
          get :oauth
        end
      end

      resources :external_services, except: [:show, :new, :edit] do
        member do
          put :update_manifest
        end
      end

      resources :schedules

      resources :contacts, except: [:show] do
        collection do
          get :search, :action => :index, :as => 'search'
          post :upload_csv
          post :import_csv
          delete :destroy_from_filter
        end
        member do
          get :calls
          get :queued_calls
        end
      end

      resources :resources do
        collection do
          get :find
        end

        resources :localized_resources do
          member do
            post :save_recording
            get :play_recording
            post :save_file
            get :play_file
          end
        end
      end

      resources :feeds

      resources :scheduled_calls do
        collection do
          post :from_filter, :action => :new, :as => 'from_filter'
        end
      end
    end
  end

  resources :call_logs, path: :calls, only: [:index, :show] do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
      get :download_details
    end
  end

  resource :synthesizer do
    get :voices
  end

  resources :alerts do
    member do
      get :dismiss
    end
  end

  namespace :api do
    match "call" => "calls#call"
    resources :calls, only: [:destroy] do
      member do
        match :state
        match :redirect
        match :cancel
      end
    end
    get "channels" => "channels#list"
    resources :channels, only: [:create] do
      collection do
        get "all", :action => "all"
        get "all/:id", :action => "get_by_id"
        get ":name", :action => "get"
        put ":name", :action => "update"
        delete ":name", :action => "destroy"
        post ":id/enable", :action => "enable"
        post ":id/disable", :action => "disable"
      end
    end
    resources :projects, only: [:index, :show] do
      resources :project_variables, only: :index
      resources :call_flows, only: [:index, :show] do
        namespace :flow_results do
          resources :packages, only: [:index, :show] do
            get "responses", :action => "responses"
          end
        end
      end
      resources :contacts do
        collection do
          get 'by_address/:address', :action => "show_by_address"
          put 'by_address/:address', :action => "update_by_address"
          put 'all', :action => "update_all"
        end
      end
      resources :schedules, only: [:index, :create] do
        collection do
          get ':name', :action => "show"
          put ':name', :action => "update"
          delete ':name', :action => "destroy"
        end
      end
    end
    resources :logs, only: [] do
      collection do
        get ':call_id', action: :list
      end
    end
  end

  get 'permissions' => 'permissions#index'
  get 'permissions/autocomplete' => 'permissions#autocomplete'
  post 'permissions/add_account' => 'permissions#add_account'
  post 'permissions/update' => 'permissions#update'

  post 'call_simulator/start'
  post 'call_simulator/resume'

  get 'oauth/google' => 'oauth#google', :as => 'google_oauth'
  match 'oauth/google/callback' => 'oauth#google_callback', :as => 'google_callback_oauth'

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('http://instedd.org/terms-of-service/')

  match '/hub/*path' => 'hub#api', format: false

  mount Listings::Engine => "/listings"
  mount InsteddTelemetry::Engine => "/instedd_telemetry"
end
